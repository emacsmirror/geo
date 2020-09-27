/* geocode-emacs.c -- Geocoding services for GNU Emacs

   Copyright (c) 2020 Would (oldosfan)
   Check the files COPYING and README for copying conditions.*/

#include "config.h"
#include "emacs-module.h"

#include <stdio.h>
#include <string.h>

#include <geocode-glib/geocode-glib.h>

#define Qnil(env) ((env)->intern ((env), "nil"))
#define Qt(env) ((env)->intern ((env), "t"))
#define Q(str, env) ((env)->intern ((env), str))
#define S(str, env) ((str) ? (env)->make_string ((env), (str), strlen (str)) : Qnil (env))

#define XGEOCODE_LOCATION(loc, env) \
  ((GeocodeLocation *) (env)->get_user_ptr ((env), (loc)))

void
plugin_is_GPL_compatible ()
{
}

static void
geocode_location_finalizer (void *value)
{
  g_object_unref (value);
}

static emacs_value
lisp_cons (emacs_env *env,
	   emacs_value a,
	   emacs_value b)
{
  return env->funcall (env, Q ("cons", env), 2, (emacs_value []) {a, b});
}

static emacs_value
make_geocode_location (emacs_env *env,
		       emacs_value latitude,
		       emacs_value longitude,
		       emacs_value accuracy)
{
  gdouble
    lat = env->extract_float (env, latitude),
    lon = env->extract_float (env, longitude),
    acc = env->extract_float (env, accuracy);

  return env->make_user_ptr (env, geocode_location_finalizer,
			     geocode_location_new (lat, lon, acc));
}

static inline emacs_value
geocode_place_to_emacs (GeocodePlace *place, emacs_env *env)
{
  emacs_value lisp_place = Qnil (env);
  lisp_place = lisp_cons (env,
			  lisp_cons (env, Q ("street", env),
				     S (geocode_place_get_street (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env,
			  lisp_cons (env, Q ("town", env),
				     S (geocode_place_get_town (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env,
			  lisp_cons (env, Q ("state", env),
				     S (geocode_place_get_state (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env,
			  lisp_cons (env, Q ("country", env),
				     S (geocode_place_get_country (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env,
			  lisp_cons (env, Q ("county", env),
				     S (geocode_place_get_county (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env,
			  lisp_cons (env, Q ("continent", env),
				     S (geocode_place_get_continent (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env,
			  lisp_cons (env, Q ("street-address", env),
				     S (geocode_place_get_street_address (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env, lisp_cons (env, Q ("building", env),
					  S (geocode_place_get_building (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env, lisp_cons (env, Q ("postal-code", env),
					  S (geocode_place_get_postal_code (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env, lisp_cons (env, Q ("country-code", env),
					  S (geocode_place_get_country_code (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env, lisp_cons (env, Q ("area", env),
					  S (geocode_place_get_area (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env, lisp_cons (env, Q ("osm-id", env),
					  S (geocode_place_get_osm_id (place), env)),
			  lisp_place);
  lisp_place = lisp_cons (env, lisp_cons (env, Q ("name", env),
					  S (geocode_place_get_name (place), env)),
			  lisp_place);
  return lisp_place;
}

static emacs_value
geocode_location_p (emacs_env *env,
		    emacs_value value)
{
  if (env->eq (env, env->funcall (env, env->intern (env, "user-ptrp"),
				  1, &value), Qnil(env)))
    return Qnil(env);
  return env->get_user_finalizer (env, value) == geocode_location_finalizer ?
    Qt(env) : Qnil (env);
}

static emacs_value
Fmake_geocode_location (emacs_env *env,
			ptrdiff_t nargs,
			emacs_value *args,
			void *data)
{
  return make_geocode_location
    (env, args[0], args[1], args[2]);
}

static emacs_value
Fgeocode_location_p (emacs_env *env,
		     ptrdiff_t nargs,
		     emacs_value *args,
		     void *data)
{
  return geocode_location_p (env, args[0]);
}

static void
Gresolve_geocode_place_cb (GObject *so,
			   GAsyncResult *res,
			   gpointer user_data)
{
  emacs_env *env = ((void **) user_data)[0];
  emacs_value *vl = ((void **) user_data)[1];
  GError **e = ((void **) user_data)[2];

  GeocodePlace *place =
    geocode_reverse_resolve_finish (GEOCODE_REVERSE (so),
				    res, e);

  if (place)
    *vl = geocode_place_to_emacs (place, env);
  g_object_unref (place);
}

static emacs_value
resolve_geocode_place (emacs_env *env,
		       emacs_value geocode_location)
{
  if (!geocode_location_p (env, geocode_location))
    env->non_local_exit_signal (env,
				Q ("wrong-type-argument", env),
				S ("geocode-location-p", env));
  GeocodeLocation *gl = XGEOCODE_LOCATION (geocode_location, env);
  GeocodeReverse *rev = geocode_reverse_new_for_location (gl);
  emacs_value val = NULL;
  GError *error = NULL;
  geocode_reverse_resolve_async (rev, NULL, Gresolve_geocode_place_cb,
				 ((void *[]) {env, &val, &error}));

  while (!val && !error)
    g_main_context_iteration (NULL, false);
  if (error)
    {
      g_error_free (error);
      env->non_local_exit_signal (env, Q ("error", env),
				  S ("Error retrieving location", env));
    }
  g_object_unref (rev);
  return val;
}

static emacs_value
Fresolve_geocode_place (emacs_env *env,
			ptrdiff_t nargs,
			emacs_value *args,
			void *data)
{
  return resolve_geocode_place (env, args[0]);
}

extern int
emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env =
    runtime->get_environment (runtime);
  emacs_value make_geocode_location =
    env->make_function (env, 3, 3, Fmake_geocode_location,
			"Create a geocode location with LAT, \
LON, and ACCURACY.\n\
\n\
(make-geocode-location LAT LON ACCURACY)", NULL);

  emacs_value geocode_location_p
    = env->make_function (env, 1, 1, Fgeocode_location_p,
			  "Return non-nil if LOCATION is a \
geocode location.\n\
\n\
(geocode-location-p LOCATION)", NULL);

  emacs_value resolve_geocode_place
    = env->make_function (env, 1, 1, Fresolve_geocode_place,
			  "Resolve reverse geocode information \
from LOCATION.\n\
\n\
(resolve-geocode-place LOCATION)", NULL);

  emacs_value Qmake_geocode_location
    = env->intern (env, "make-geocode-location");
  emacs_value Qgeocode_location_p
    = env->intern (env, "geocode-location-p");
  emacs_value Qresolve_geocode_place
    = env->intern (env, "resolve-geocode-place");
  env->funcall (env, env->intern (env, "defalias"), 2,
		(emacs_value []) {Qmake_geocode_location, make_geocode_location});
  env->funcall (env, env->intern (env, "defalias"), 2,
		(emacs_value []) {Qgeocode_location_p, geocode_location_p});

  env->funcall (env, env->intern (env, "defalias"), 2,
		(emacs_value []) {Qresolve_geocode_place, resolve_geocode_place});

  env->funcall (env, env->intern (env, "provide"), 1,
		(emacs_value []) {env->intern (env, "geocode-emacs")});
  return 0;
}
