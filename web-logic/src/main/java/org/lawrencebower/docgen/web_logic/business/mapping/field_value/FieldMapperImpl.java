package org.lawrencebower.docgen.web_logic.business.mapping.field_value;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.mapping.field_value.FieldMapper;
import org.lawrencebower.docgen.web_model.business_def.utils.ViewUtils;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class FieldMapperImpl implements FieldMapper {

    @Autowired
    ViewUtils viewUtils;

    private static Set<String> EXCLUDED_TOKENS;

    static {
        EXCLUDED_TOKENS = new HashSet<>();
        EXCLUDED_TOKENS.add("full");
        EXCLUDED_TOKENS.add("partial");
    }

    @Override
    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap,
                                           List<DocComponentView> allViewComponents) {

        for (String key : parameterMap.keySet()) {

            if (isExcludedToken(key)) {
                continue;
            }

            extractAndSetValues(parameterMap,
                                allViewComponents,
                                key);
        }

    }

    private void extractAndSetValues(Map<String, String[]> parameterMap,
                                     List<DocComponentView> allViewComponents,
                                     String componentName) {

        String componentValue = getParameterValue(componentName, parameterMap);

        for (DocComponentView component : allViewComponents) {
            component.checkAndSetValueFromParamString(componentName, componentValue);
        }
    }

    private String getParameterValue(String key, Map<String, String[]> parameterMap) {

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

    @Override
    public boolean isExcludedToken(String token) {
        return EXCLUDED_TOKENS.contains(token);
    }
}
