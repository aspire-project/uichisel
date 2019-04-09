extern char *optarg; 
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
  p = tmp;
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
