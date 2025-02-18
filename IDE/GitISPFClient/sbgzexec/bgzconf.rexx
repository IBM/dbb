/* REXX */
/*%STUB CALLCMD*/
/*********************************************************************/
/*                                                                   */
/* IBM ISPF Git Interface                                            */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/* NAME := BGZSTART                                                  */
/*                                                                   */
/* DESCRIPTIVE NAME := ISPF Git Client configuration                 */
/*                                                                   */
/* FUNCTION := The ISPF Client configuration module sets up the      */
/*             location of key components such as the location of    */
/*             Java and the Rocket products.                         */
/*                                                                   */
/* CALLED BY : BGZSTART - ISPF Git Client startup module             */
/*                                                                   */
/* INSTRUCTIONS : 1. Change the value assigned to JAVA_HOME to the   */
/*                   location of Java on your system.                */
/*                2. Change the value assigned to Rocket_HOME to the */
/*                   location where you have installed the Rocket    */
/*                   products.                                       */
/*                3. If using DBB, change the value assigned to      */
/*                   DBB_HOME to the location where you have         */
/*                   installed DBB.                                  */
/*                4. Enter the location of iconv on your system. By  */
/*                   default this is /bin/iconv.                     */
/*                                                                   */
/* PARAMETERS                                                        */
/*                                                                   */
/* OUTPUT := None                                                    */
/*                                                                   */
/* Change History                                                    */
/*                                                                   */
/* Who   When     What                                               */
/* ----- -------- -------------------------------------------------- */
/* LD    14/05/19 Initial version                                    */
/* LD    11/09/24 Add some new environment variables                 */
/*                                                                   */
/*********************************************************************/

  Address ISPEXEC

  JAVA_HOME    = '/usr/lpp/java/J11.0_64'
  Git_HOME     = '/var/rocket'
  Bash_HOME    = '/var/rocket'
  Gitcore_HOME = '/var/rocket'
  Man_HOME     = '/var/rocket'
  Perl5_HOME   = '/var/rocket'
  DBB_HOME     = '/var/dbb200'
  DBB_CONF     = '/etc/dbbv2'
  ICONV_HOME   = '/bin/iconv'
  CAINFO       = 'export GIT_SSL_CAINFO=/var/rocket/cacert.pem'
  Daemon_Host  = '127.0.0.1'
  Daemon_Port  = '8080'

  BGZJAVAH = JAVA_HOME
  BGZGITH  = Git_HOME
  BGZBASH  = Bash_HOME
  BGZCGIT  = Gitcore_HOME
  BGZMAN   = Man_HOME
  BGZPERL5 = Perl5_HOME
  BGZDBBH  = DBB_HOME
  BGZDBBC  = DBB_CONF
  BGZICONV = ICONV_HOME
  BGZCAINF = CAINFO
  BGZDMNHO = Daemon_Host
  BGZDMNPO = Daemon_Port

  'VPUT (BGZJAVAH,BGZGITH,BGZBASH,BGZCGIT,BGZMAN,BGZPERL5) SHARED'
  'VPUT (BGZDBBH,BGZDBBC,BGZICONV,BGZCAINF) SHARED'
  'VPUT (BGZDMNHO,BGZDMNPO) SHARED'

Exit
