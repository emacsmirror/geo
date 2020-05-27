/* geocode-emacs.c -- Geocoding services for GNU Emacs

   Copyright (c) 2020 Would (oldosfan)
   Check the files COPYING and README for copying conditions.*/

#include "config.h"
#include "emacs-module.h"

#include <stdio.h>

#include <geocode-glib/geocode-glib.h>

#define Qnil(env) ((env)->intern ((env), "nil"))
#define Qt(env) ((env)->intern ((env), "t"))

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

extern int
emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env =
    runtime->get_environment (runtime);
  emacs_value make_geocode_location =
    env->make_function (env, 3, 3, Fmake_geocode_location,
			"Create a geocode location with LAT, \
LON, and ACCURACY.\n\
usage: (make-geocode-location LAT LON ACCURACY)", NULL);

  emacs_value geocode_location_p
    = env->make_function (env, 1, 1, Fgeocode_location_p,
			  "Return non-nil if LOCATION is a \
geocode location.\n\
usage: (geocode-location-p LOCATION)", NULL);

  emacs_value Qmake_geocode_location
    = env->intern (env, "make-geocode-location");
  emacs_value Qgeocode_location_p
    = env->intern (env, "geocode-location-p");
  env->funcall (env, env->intern (env, "defalias"), 2,
		(emacs_value []) {Qmake_geocode_location, make_geocode_location});
  env->funcall (env, env->intern (env, "defalias"), 2,
		(emacs_value []) {Qgeocode_location_p, geocode_location_p});
  return 0;
}
