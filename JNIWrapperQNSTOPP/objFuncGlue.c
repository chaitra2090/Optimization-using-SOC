// The following program objfuncGlue.c is the glue-code in C that holds Fortran and 
// Java together.

#include <stdio.h>
#include <stdlib.h>
#include <jni.h>  

extern double obj(double *, int *);
extern int initialize(); 
// Denotes a Java VM
static JavaVM *jvm;

JNIEnv* create_vm(JavaVM **jvm)
{
	// Pointer to native method interface
    JNIEnv* env;
    //  JDK/JRE VM initialization arguments
    JavaVMInitArgs args;
    JavaVMOption options;
    args.version = JNI_VERSION_1_6;
    args.nOptions = 1;
    options.optionString = "-Djava.class.path=./";
    args.options = &options;
    args.ignoreUnrecognized = 0;
    int flag;
    // Load and initialize a Java VM, return a JNI interface pointer in env
    flag = JNI_CreateJavaVM(jvm, (void**)&env, &args);
    if (rv < 0 || !env)
        printf("Unable to Launch JVM %d\n",rv);
    else
        printf("Launched JVM successfully\n");
        
    return env;
}

int initialize()
{
	// Call to create a JVM
	JNIEnv *env_init;
	env_init = create_vm(&jvm);
	if(env_init == NULL)
  		return 1;
}

void objP(double *c_ptr, int *c_size, int *p, double *r_ptr) {
	// JNI interface pointer
	JNIEnv *env;
	// Local variables
	jdouble *dvarsPtr;
	jdouble *respPtr;
	jdoubleArray dvars;
	jint size;
	jint P;
	int k;
	int length;
	double *respVars;
	
	/* Dereference pointers
	size = *c_size;
	dvarsPtr = (double *)c_ptr;
	respPtr = (double *)resp_ptr;
	P = *p;
	
	/*
	Allow the current thread to attach itself to the JVM and obtain a
	JNI interface pointer.
	*/
	jint flag = (*jvm)->AttachCurrentThread(jvm, (void **)&env, NULL);
	if (flag < 0 || !env)
		printf("Unable to attach to the existing instance of JVM %d",flag);
		exit(1);
	else
		printf("Attached to the existing JVM thread");
	
	// Construct a new primitive array object
	dvars = (*env)->NewDoubleArray(env, size);
	(*env)->SetDoubleArrayRegion(env, dvars, 0, size, (const
	jdouble*)dvarsPtr);
	
	// Local variables for invocation of Java classes and methods
	jclass objFuncJ_class;
	jmethodID main_method;
	jmethodID evaluate_method;
	
	// Find Java class by name
	objFuncJ_class = (*env)->FindClass(env, "objFuncJ");

	// Find Java method by name and signature
	evaluate_method = (*env)->GetStaticMethodID(env, objFuncJ_class, "evaluateP", "([DI)[D");
	
	// Invoke the objFuncJ.evaluate method using the JNI
	jdoubleArray respArray = (*env)->CallStaticObjectMethod(env,
	objFuncJ_class, evaluate_method, dvars, P);
	if(respArray != NULL) {
		length = (*env)->GetArrayLength(env, respArray);
		respVars = (*env)->GetDoubleArrayElements(env, respArray, NULL);
	}
	else
		for (k = 0; k < length; k++) {
			respPtr[k] = respVars[k];
	}
	// Release elements of the array
	(*env)->ReleaseDoubleArrayElements(env, respArray, value, 0);
	return ;
}



