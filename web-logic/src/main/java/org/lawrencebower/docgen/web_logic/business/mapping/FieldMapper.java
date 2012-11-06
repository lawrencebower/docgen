package org.lawrencebower.docgen.web_logic.business.mapping;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;

import java.util.*;

public class FieldMapper {

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

        List<DocComponentView> components = getComponentsWithName(componentName, documents);

        String value = getFieldValue(componentName, parameterMap.get(componentName));

        for (DocComponentView component : components) {
            component.setComponentFromParamString(value);
        }

    }

    private String getFieldValue(String key, String[] strings) {

        if (strings.length == 0) {
            throw new DocGenException("No values bound to field - " + key);
        }

        if (strings.length > 1) {
            throw new DocGenException("more than 1 value bound to field - " + key);
        }

        return strings[0];
    }

    private List<DocComponentView> getComponentsWithName(String componentName,
                                                         List<DocumentInfoView> documents) {

        List<DocComponentView> results = new ArrayList<>();

        for (DocumentInfoView document : documents) {
            List<DocComponentView> matchingComponents = document.getComponentsWithName(componentName);
            results.addAll(matchingComponents);
        }

        return results;
    }

    public boolean isExcludedToken(String token) {
        return EXCLUDED_TOKENS.contains(token);
    }
}
