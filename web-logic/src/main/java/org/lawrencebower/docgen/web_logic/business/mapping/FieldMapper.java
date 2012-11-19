package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.utils.ViewUtils;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

public class FieldMapper {

    @Autowired
    ViewUtils viewUtils;

    private static Set<String> EXCLUDED_TOKENS;

    static {
        EXCLUDED_TOKENS = new HashSet<>();
        EXCLUDED_TOKENS.add("full");
        EXCLUDED_TOKENS.add("partial");
    }

    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap,
                                           List<DocumentInfoView> documents) {

        for (String key : parameterMap.keySet()) {

            if (isExcludedToken(key)) {
                continue;
            }

            extractAndSetValues(parameterMap,
                                documents,
                                key);
        }

    }

    private void extractAndSetValues(Map<String, String[]> parameterMap,
                                     List<DocumentInfoView> documents,
                                     String componentName) {

        List<DocComponentView> allViewComponents = viewUtils.getAllComponentViewsFromDocs(documents);

        String componentValue = getComponentValue(componentName, parameterMap);

        for (DocComponentView component : allViewComponents) {
            component.checkAndSetValueFromParamString(componentName, componentValue);
        }
    }

    private String getComponentValue(String key, Map<String, String[]> parameterMap) {

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
