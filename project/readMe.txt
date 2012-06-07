This is just a hint on how to call SBT.
Among others, in my SBT runner script I supply the following JVM parameters:

-Xmx1024M
-Xss30M
-XX:MaxPermSize=300M
-XX:ReservedCodeCacheSize=128M