#include <stdio.h>

int main( int argc, char *argv[], char *env[] ) {
   int i;

   fprintf( stderr, "Environment...\n" );
   for( i = 0; env[ i ]; i++ ) {
      fprintf( stderr, "env[%d]: %s\n", i, env[ i ] );
   }
   
   fprintf( stdout, "Arguments...\n" );
   for( i = 0; i < argc; i++ ) {
      fprintf( stdout, "argv[%d]: %s\n", i, argv[ i ] );
   }
}
