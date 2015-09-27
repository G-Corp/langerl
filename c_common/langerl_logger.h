#ifndef __LANGERL_LOGGER_INCLUDE
#define __LANGERL_LOGGER_INCLUDE

#ifdef LANGERL_DEBUG
#define LANGERL_LOG(...) log_print(__FILE__, __LINE__, __VA_ARGS__ )
void log_print(char* filename, int line, char *fmt, ...);
#else
#define LANGERL_LOG(...) 
#endif

#endif

