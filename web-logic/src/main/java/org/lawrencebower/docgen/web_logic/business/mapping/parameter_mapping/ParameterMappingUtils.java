package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ParameterMappingUtils {

    @Autowired
    private ViewUtils viewUtils;

    private static Set<String> EXCLUDED_TOKENS;

    static {
        EXCLUDED_TOKENS = new HashSet<>();
        EXCLUDED_TOKENS.add("full");
    }

    public String getParameterValue(String key, Map<String, String[]> parameterMap) {

        viewUtils.checkNullArg(key, parameterMap);

        String[] componentValues = parameterMap.get(key);

        if (componentValues.length == 0) {
            String message = String.format("No values bound to field '%s'", key);
            throw new DocGenException(message);
        }

        if (componentValues.length > 1) {
            String message = String.format("more than 1 value bound to field '%s'", key);
            throw new DocGenException(message);
        }

        return componentValues[0];
    }

    public boolean isExcludedToken(String token) {
        return EXCLUDED_TOKENS.contains(token);
    }

    public String[] splitStringBySeparator(String value) {

        viewUtils.checkNullArg(value);

        String[] tokens = value.split(ViewConstants.DOCUMENT_FIELD_SEPARATOR);

        return tokens;
    }
}
