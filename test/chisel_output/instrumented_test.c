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
  {
#line 9
  __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 9
  fprintf(__tmp_descr, "%d %s %d %d\n", 9, "test.c", 353, owner != 4294967295U);
#line 9
  fflush(__tmp_descr);
#line 9
  fclose(__tmp_descr);
#line 9
  if (owner != 4294967295U) {
#line 10
    goto _L___1;
  } else {
    {
#line 12
    __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 12
    fprintf(__tmp_descr, "%d %s %d %d\n", 12, "test.c", 408, group != 4294967295U);
#line 12
    fflush(__tmp_descr);
#line 12
    fclose(__tmp_descr);
#line 12
    if (group != 4294967295U) {
      _L___1: 
      {
#line 13
      tmp = chown((char const   *)basename_dir, owner, group);
      }
      {
#line 14
      __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 14
      fprintf(__tmp_descr, "%d %s %d %d\n", 14, "test.c", 507, tmp);
#line 14
      fflush(__tmp_descr);
#line 14
      fclose(__tmp_descr);
#line 14
      if (tmp) {
        {
#line 16
        exit(-1);
        }
      }
      }
    }
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
  {
#line 27
  __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 27
  fprintf(__tmp_descr, "%d %s %d %d\n", 27, "test.c", 657, sizeof(int ) != 4UL);
#line 27
  fflush(__tmp_descr);
#line 27
  fclose(__tmp_descr);
#line 27
  if (sizeof(int ) != 4UL) {
#line 28
    return (0);
  }
  }
  {
#line 30
  __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 30
  fprintf(__tmp_descr, "%d %s %d %d\n", 30, "test.c", 792, sizeof(short ) != 2UL);
#line 30
  fflush(__tmp_descr);
#line 30
  fclose(__tmp_descr);
#line 30
  if (sizeof(short ) != 2UL) {
#line 31
    return (0);
  }
  }
  {
#line 33
  __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 33
  fprintf(__tmp_descr, "%d %s %d %d\n", 33, "test.c", 927, sizeof(char ) != 1UL);
#line 33
  fflush(__tmp_descr);
#line 33
  fclose(__tmp_descr);
#line 33
  if (sizeof(char ) != 1UL) {
#line 34
    return (0);
  }
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
  {
#line 56
  __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 56
  fprintf(__tmp_descr, "%d %s %d %d\n", 56, "test.c", 1323, c);
#line 56
  fflush(__tmp_descr);
#line 56
  fclose(__tmp_descr);
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
  {
#line 81
  __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 81
  fprintf(__tmp_descr, "%d %s %d %d\n", 81, "test.c", 1600, tmp___2 == 0);
#line 81
  fflush(__tmp_descr);
#line 81
  fclose(__tmp_descr);
#line 81
  tmp___2 = bz_config_ok();
  }
  }
  {
#line 81
  __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 81
  fprintf(__tmp_descr, "%d %s %d %d\n", 81, "test.c", 1600, tmp___2 == 0);
#line 81
  fflush(__tmp_descr);
#line 81
  fclose(__tmp_descr);
#line 81
  if (tmp___2 == 0) {
    {
#line 81
    exit(1);
    }
  }
  }
  {
#line 82
  __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 82
  fprintf(__tmp_descr, "%d %s %d %d\n", 82, "test.c", 1640, argc == 1);
#line 82
  fflush(__tmp_descr);
#line 82
  fclose(__tmp_descr);
#line 82
  if (argc == 1) {
    {
#line 84
    __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 84
    fprintf(__tmp_descr, "%d %s %d %d\n", 84, "test.c", 1665, tmp > 0);
#line 84
    fflush(__tmp_descr);
#line 84
    fclose(__tmp_descr);
#line 84
    if (tmp > 0) {
      {
#line 85
      tmp___0 = f();
      }
    }
    }
#line 86
    a = tmp___0;
#line 87
    c ++;
  } else {
    {
#line 90
    __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 90
    fprintf(__tmp_descr, "%d %s %d %d\n", 90, "test.c", 1743, argc == 2);
#line 90
    fflush(__tmp_descr);
#line 90
    fclose(__tmp_descr);
#line 90
    if (argc == 2) {
#line 91
      b = 0;
#line 92
      goto _L;
    } else {
      {
#line 94
      __tmp_descr = (void *)fopen("chisel_output/__tmp_file", "a");
#line 94
      fprintf(__tmp_descr, "%d %s %d %d\n", 94, "test.c", 1792, argc == 3);
#line 94
      fflush(__tmp_descr);
#line 94
      fclose(__tmp_descr);
#line 94
      if (argc == 3) {
        _L: 
#line 96
        b = 1;
      } else {
#line 98
        b = 2;
      }
      }
    }
    }
  }
  }
  {
#line 101
  tmp___1 = g(b + c);
  }
#line 103
  return (0);
}
}
