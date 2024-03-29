/*
KeywordArgs: Annotation used to annotate constructors that use 
keyword arguments and specify default values for those arguments
*/

package ist.meic.pa;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.annotation.ElementType;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.CONSTRUCTOR)
public @interface KeywordArgs {
	String value();
}
