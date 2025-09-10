#if defined(__GFORTRAN__)
#define assertm(cond) assert((cond), "cond")
#define assertm_not(cond) assert_not((cond), ".NOT. " // "cond")
#elif defined(NAGFOR)
#define assertm(cond) assert((cond), "cond")
#define assertm_not(cond) assert_not((cond), ".NOT. " // "cond")
#else
#define assertm(cond) assert((cond), #cond)
#define assertm_not(cond) assert((cond), ".NOT. " // #cond)
#endif