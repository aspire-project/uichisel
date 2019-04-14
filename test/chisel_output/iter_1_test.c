struct _IO_FILE;
extern int fclose(struct _IO_FILE *__stream ) ;
struct _IO_FILE;
extern int fflush(struct _IO_FILE *__stream ) ;
struct _IO_FILE;
extern struct _IO_FILE *fopen(char const   * __restrict  __filename , char const   * __restrict  __modes ) ;
struct _IO_FILE;
extern int fprintf(struct _IO_FILE * __restrict  __stream , char const   * __restrict  __format 
                   , ...) ;
extern  __attribute__((__nothrow__)) int ( __attribute__((__nonnull__(1), __leaf__)) chown)(char const   *__file ,
                                                                                            unsigned int __owner ,
                                                                                            unsigned int __group ) ;
#line 6 "test.c"
int make_path(unsigned int owner , unsigned int group ) 
{ 
  int tmp ;
  char *basename_dir ;
  void *__tmp_descr ;
  char *__cil_tmp6 ;

  {
#line 8
  basename_dir = (char *)"";
#line 9
  if (owner != 4294967295U) {
#line 10
    goto _L___1;
  } else
#line 12
  if (group != 4294967295U) {
    _L___1: 
    {
#line 13
    tmp = chown((char const   *)basename_dir, owner, group);
    }
#line 14
    if (tmp) {
      {
#line 16
      exit(-1);
      }
    }
  }
#line 21
  return (0);
}
}
#line 24 "test.c"
static int bz_config_ok(void) 
{ 
  void *__tmp_descr ;

  {
#line 27
  if (sizeof(int ) != 4UL) {
#line 28
    return (0);
  }
#line 30
  if (sizeof(short ) != 2UL) {
#line 31
    return (0);
  }
#line 33
  if (sizeof(char ) != 1UL) {
#line 34
    return (0);
  }
#line 36
  return (1);
}
}
#line 39 "test.c"
int z(void) 
{ 
  void *__tmp_descr ;

  {
#line 39
  return (1);
}
}
#line 40 "test.c"
int (*p)(void)  =    & z;
#line 41 "test.c"
int f(void) 
{ 
  int a ;
  void *__tmp_descr ;

  {
#line 47
  a = 1;
#line 48
  return (a);
}
}
#line 51 "test.c"
int g(int c ) 
{ 
  void *__tmp_descr ;

  {
#line 56
  if (c) {
#line 57
    return (1);
  } else {
#line 59
    return (2);
  }
}
}
#line 63 "test.c"
int main(int argc , char **argv ) 
{ 
  int p___0 ;
  int tmp ;
  int c ;
  int b ;
  int a ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  void *__tmp_descr ;

  {
  {
#line 76
  tmp = rand();
#line 77
  p___0 = make_path(1U, 1U);
#line 78
  c = 0;
#line 79
  b = 0;
#line 81
  tmp___2 = bz_config_ok();
  }
#line 81
  if (tmp___2 == 0) {
    {
#line 81
    exit(1);
    }
  }
#line 82
  if (argc == 1) {
#line 84
    if (tmp > 0) {
      {
#line 85
      tmp___0 = f();
      }
    }
#line 86
    a = tmp___0;
#line 87
    c ++;
  } else
#line 90
  if (argc == 2) {
#line 91
    b = 0;
#line 92
    goto _L;
  } else
#line 94
  if (argc == 3) {
    _L: 
#line 96
    b = 1;
  } else {
#line 98
    b = 2;
  }
  {
#line 101
  tmp___1 = g(b + c);
  }
#line 103
  return (0);
}
}
