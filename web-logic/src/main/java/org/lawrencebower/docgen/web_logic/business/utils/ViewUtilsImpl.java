package org.lawrencebower.docgen.web_logic.business.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;

public class ViewUtilsImpl implements ViewUtils {

    @Override
    public String toHTMLString(String string) {
        return string.replaceAll("\n","<br/>");
    }

    @Override
    public void checkNullArg(Object... args){
        for (Object arg : args) {
            if(arg == null){
                throw new DocGenException("Argument is null");
            }
        }
    }
}
