#include <assert.h>
#if !defined(PHREEQC_CLASS)
#define EXTERNAL extern
#include "global.h"
#else
#include "Phreeqc.h"
#endif
#include <setjmp.h>
#include "output.h"
#include "phrqproto.h"
#include "phqalloc.h"
static char const svnid[] =
	"$Id: output.c 3873 2009-12-08 00:56:50Z dlpark $";
#if !defined(PHREEQC_CLASS)
#define MAX_CALLBACKS 10
static struct output_callback output_callbacks[MAX_CALLBACKS];
static size_t count_output_callback = 0;
static int forward_output_to_log = 0;
#endif

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
add_output_callback(PFN_OUTPUT_CALLBACK pfn, void *cookie)
/* ---------------------------------------------------------------------- */
{
	if (svnid == NULL)
		fprintf(stderr, " ");
	if (pfn)
	{
		if (count_output_callback >= MAX_CALLBACKS - 1)
		{
			sprintf(error_string, "Too many callbacks.\nSee %s\n", __FILE__);
			fprintf(stderr, "%s", error_string);
			error_msg(error_string, STOP);
			return RLIB_ERROR;
		}
		output_callbacks[count_output_callback].callback = pfn;
		output_callbacks[count_output_callback].cookie = cookie;
		++count_output_callback;
	}
	return OK;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_message(const int type, const char *err_str, const int stop,
			   const char *format, va_list args)
/* ---------------------------------------------------------------------- */
{
#if !defined(PHREEQC_CLASS)
	extern jmp_buf mark;
#endif
	size_t i;

	for (i = 0; i < count_output_callback; ++i)
	{
#ifdef VACOPY
		va_list args_copy;
		va_copy(args_copy, args);
		(output_callbacks[i].callback) (ACTION_OUTPUT, type, err_str, stop,
										output_callbacks[i].cookie, format,
										args_copy);
		va_end(args_copy);
#else
		(output_callbacks[i].callback) (ACTION_OUTPUT, type, err_str, stop,
										output_callbacks[i].cookie, format,
										args);
#endif
	}

	if (stop == STOP)
	{
		longjmp(mark, input_error);
	}
	return OK;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
clean_up_output_callbacks(void)
/* ---------------------------------------------------------------------- */
{
	count_output_callback = 0;
	return OK;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
error_msg(const char *err_str, const int stop, ...)
/* ---------------------------------------------------------------------- */
{
	va_list args;
	int return_value;

	if (input_error <= 0)
		input_error = 1;
	va_start(args, stop);
	return_value = output_message(OUTPUT_RLIB_ERROR, err_str, stop, "", args);
	va_end(args);
	return (return_value);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
warning_msg(const char *err_str, ...)
/* ---------------------------------------------------------------------- */
{
	va_list args;
	int return_value;

	va_start(args, err_str);
	return_value =
		output_message(OUTPUT_WARNING, err_str, CONTINUE, "", args);
	count_warnings++;
	va_end(args);
	return (return_value);
}

// MDL: change output!
/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_msg(const int type, const char *format, ...)
/* ---------------------------------------------------------------------- */
{
  int return_value;
  va_list args;

  if (R_fileprint == FALSE)
    return(OK);
  else
    {
      va_start (args, format);
      return_value = output_message (type, NULL, CONTINUE, format, args);
      va_end (args);
    }
  return (return_value);
}

/* ---------------------------------------------------------------------- */
void CLASS_QUALIFIER
set_forward_output_to_log(int value)
/* ---------------------------------------------------------------------- */
{
	forward_output_to_log = value;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
get_forward_output_to_log(void)
/* ---------------------------------------------------------------------- */
{
	return forward_output_to_log;
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_fflush(const int type, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	int check;
	va_list args;

	check = OK;
	va_start(args, type);
	for (i = 0; i < count_output_callback; ++i)
	{
		check =
			(output_callbacks[i].callback) (ACTION_FLUSH, type, NULL,
											CONTINUE,
											output_callbacks[i].cookie, NULL,
											args);
		if (check != OK)
			break;
	}
	va_end(args);
	if (check != OK)
		return (RLIB_ERROR);
	return (OK);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_rewind(const int type, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	int check;
	va_list args;

	check = OK;
	va_start(args, type);
	for (i = 0; i < count_output_callback; ++i)
	{
		check =
			(output_callbacks[i].callback) (ACTION_REWIND, type, NULL,
											CONTINUE,
											output_callbacks[i].cookie, NULL,
											args);
		if (check != OK)
			break;
	}
	va_end(args);
	if (check != OK)
		return (RLIB_ERROR);
	return (OK);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_close(const int type, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	int check;
	va_list args;

	check = OK;
	va_start(args, type);
	for (i = 0; i < count_output_callback; ++i)
	{
		check =
			(output_callbacks[i].callback) (ACTION_CLOSE, type, NULL,
											CONTINUE,
											output_callbacks[i].cookie, NULL,
											args);
		if (check != OK)
			break;
	}
	va_end(args);
	if (check != OK)
		return (RLIB_ERROR);
	return (OK);
}

/* ---------------------------------------------------------------------- */
int CLASS_QUALIFIER
output_open(const int type, const char *file_name, ...)
/* ---------------------------------------------------------------------- */
{
	size_t i;
	int check;
	va_list args;
	assert(file_name && strlen(file_name));

	check = OK;
	va_start(args, file_name);
	for (i = 0; i < count_output_callback; ++i)
	{
		check =
			(output_callbacks[i].callback) (ACTION_OPEN, type, file_name,
											CONTINUE,
											output_callbacks[i].cookie, NULL,
											args);
		if (check != OK)
			break;
	}
	va_end(args);
	if (check != OK)
		return (RLIB_ERROR);
	return (OK);
}
