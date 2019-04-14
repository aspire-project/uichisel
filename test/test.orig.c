extern char *optarg;
extern
    __attribute__((__nothrow__)) int(__attribute__((__nonnull__(1), __leaf__))
                                     chown)(char const *__file, unsigned int __owner,
                                            unsigned int __group);
int make_path(unsigned int owner, unsigned int group) {
	int tmp;
	char* basename_dir = "";
	if (owner != 4294967295U) {
		goto _L___1;
	} else {
		if (group != 4294967295U) {
_L___1 : { tmp = chown((char const *)basename_dir, owner, group); }
		 if (tmp) {
			 {
				 exit(-1);
			 }
		 }
		}
	}
}
static int owner = 0;
static int group = 0;
static int bz_config_ok(void) {             

  {
    if (sizeof(int) != 4UL) {               
      return (0);                           
    }                                       
    if (sizeof(short) != 2UL) {             
      return (0);                           
    }                                       
    if (sizeof(char) != 1UL) {              
      return (0);                           
    }                                       
    return (1);                             
  }                                         
}
int z(void) { return 1;}
int (*p)(void) = &z;
int f(void) 
{ 
  int a ;
  void *__tmp_descr ;

  {
  a = 1;
  return (a);
}
}
int g(int c ) 
{ 
  void *__tmp_descr ;

  {
  if (c) {
    return (1);
  } else {
    return (2);
  }
}
}
int main(int argc , char **argv ) 
{ 
  int p ;
  int tmp ;
  int c ;
  int b ;
  int a ;
  int tmp___0 ;
  int tmp___1 ;
  void *__tmp_descr ;
  
  {
  {
  tmp = rand();
  p = make_path(1,1);
  c = 0;
  b = 0;
  }
  if (bz_config_ok() == 0) { exit(1); }
  if (argc == 1) {
    {
		if (tmp > 0)
		    tmp___0 = f();
    a = tmp___0;
    c ++;
    }
  } else
  if (argc == 2) {
    b = 0;
	goto _L;
  } else
  if (argc == 3) {
_L:
    b = 1;
  } else {
    b = 2;
  }
  {
  tmp___1 = g(b + c);
  }
  return 0;
}
}
