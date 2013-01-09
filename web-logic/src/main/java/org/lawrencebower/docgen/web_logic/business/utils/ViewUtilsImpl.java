package org.lawrencebower.docgen.web_logic.business.utils;

import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;

public class ViewUtilsImpl implements ViewUtils {

    @Override
    public String toHTMLString(String string) {
        return string.replaceAll("\n","<br/>");
    }
}
