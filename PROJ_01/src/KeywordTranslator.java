import javassist.translator;

public class KeywordTranslator implements Translator{
	
	public void start(ClassPool pool) throws NotFoundException, CannotCompileException {
	
	}
	
	public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
		System.out.println("CLASS LOADED:" + className);
		
		// CtClass ctClass = pool.get(className);
		// try {
		// 	memoizeMethods(ctClass);
		// } catch (ClassNotFoundException e) {
		// 	throw new RuntimeException(e);
		// }
	}
}