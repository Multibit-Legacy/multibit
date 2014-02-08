#
# This ProGuard configuration file illustrates how to process applications.
# Usage:
#     java -jar proguard.jar @applications.pro
#

# Specify the input jars, output jars, and library jars.

-injars  ../../../target/multidoge-exe-full.jar
-outjars ../../../target/multidoge-exe.jar

-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/rt.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/alt-rt.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/charsets.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/jce.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/jfr.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/JObjC.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/jsse.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/management-agent.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/ext/dnsns.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/ext/localedata.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/ext/sunec.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/ext/sunjce_provider.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/ext/sunpkcs11.jar
-libraryjars ../skeleton/mac/MultiDoge.app/Contents/Plugins/jdk1.7.0_25.jdk/Contents/Home/jre/lib/ext/zipfs.jar

# Save the obfuscation mapping to a file, so you can de-obfuscate any stack
# traces later on. Keep a fixed source file attribute and all line number
# tables to get line numbers in the stack traces.
# You can comment this out if you're not interested in stack traces.

-dontobfuscate
-dontoptimize
-ignorewarnings

#-printmapping out.map
#-renamesourcefileattribute SourceFile
#-keepattributes SourceFile,LineNumberTable

# Preserve all annotations.
-keepattributes *Annotation*
-keepattributes *Path*
-keepdirectories

# You can print out the seeds that are matching the keep options below.

#-printseeds out.seeds

# Preserve the MultiDoge app
-keep public class org.multibit.MultiBitInExecutableJar {
    public static void main(java.lang.String[]);
}

# Preserve all native method names and the names of their classes.

-keepclasseswithmembernames class * {
    native <methods>;
}

# Preserve the special static methods that are required in all enumeration
# classes.

-keepclassmembers class * extends java.lang.Enum {
    public static **[] values();
    public static ** valueOf(java.lang.String);
}

# Explicitly preserve all serialization members. The Serializable interface
# is only a marker interface, so it wouldn't save them.
# You can comment this out if your application doesn't use serialization.
# If your code contains serializable classes that have to be backward 
# compatible, please refer to the manual.

-keepclassmembers class * implements java.io.Serializable {
    static final long serialVersionUID;
    static final java.io.ObjectStreamField[] serialPersistentFields;
    private void writeObject(java.io.ObjectOutputStream);
    private void readObject(java.io.ObjectInputStream);
    java.lang.Object writeReplace();
    java.lang.Object readResolve();
}

# Your application may contain more items that need to be preserved; 
# typically classes that are dynamically created using Class.forName:
-keep public class com.xeiam.xchange.**
{
 *;
}
-keep class com.xeiam.xchange.**
{
 *;
}
-keep public class org.multibit.**
{
 *;
}
-keep class org.multibit.**
{
 *;
}

-keep public class com.piuk.blockchain.**
-keep class com.piuk.blockchain.**

-keep public class com.google.dogecoin.**
{
 *;
}
-keep class com.google.dogecoin.**
{
 *;
}

-keep public class org.dogecoin.**
-keep public class org.dogecoinj.**
-keep public class org.simplericity.macify.**
-keep public class sun.misc.Cleaner
-keep public class ch.qos.**
-keep public class java.nio.**
-keep public class si.mazi.**
-keep public class com.fasterxml.jackson.**
-keep public class org.java_websocket.**

-keep public class javax.ws.**
{
 *;
}
-keep class javax.ws.**
{
 *;
}

-keep public class org.slf4j.**
-keep class org.slf4j.**

-keep public class org.xml.sax.**
-keep class org.xml.sax.**

-keep public class org.joda.time.**
-keep public class org.joda.money.**
-keep public class com.google.common.base.**
-keep public class com.google.common.core.**
-keep public class com.google.common.collect.**
-keep public class com.google.common.util.concurrent.**
-keep public class com.google.protobuf.**
-keep public class com.google.zxing.qrcode.**
-keep public class org.jboss.netty.**
-keep public class org.spongycastle.crypto.**
-keep public class org.spongycastle.math.**

-keep public interface org.slf4j.**
-keep public interface org.xml.sax.**
-keep public interface com.xeiam.xchange.**
-keep public interface org.multibit.**
-keep public interface com.piuk.blockchain.**
-keep public interface com.google.dogecoin.**
-keep public interface org.dogecoin.**
-keep public interface org.dogecoinj.**
-keep public interface org.simplericity.macify.**
-keep public interface si.mazi.**
-keep public interface javax.ws.**
-keep public interface org.joda.money.**
-keep public interface org.joda.time.**
-keep public interface com.google.common.base.**
-keep public interface com.google.common.core.**
-keep public interface com.google.common.collect.**
-keep public interface com.google.common.util.concurrent.**
-keep public interface com.google.protobuf.**
-keep public interface com.google.zxing.qrcode.**
-keep public interface org.jboss.netty.**

