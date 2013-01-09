package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ParameterMappingUtils {

    private static Set<String> EXCLUDED_TOKENS;

    static {
        EXCLUDED_TOKENS = new HashSet<>();
    }

    public String getParameterValue(String key, Map<String, String[]> parameterMap) {

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

}
