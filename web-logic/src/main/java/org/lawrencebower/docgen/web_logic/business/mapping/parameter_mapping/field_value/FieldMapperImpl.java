package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.field_value;

import org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.ParameterMappingUtils;
import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.field_value.FieldMapper;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.Map;

public class FieldMapperImpl implements FieldMapper {

    @Autowired
    ParameterMappingUtils parameterMappingUtils;

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
        return parameterMappingUtils.getParameterValue(key, parameterMap);
    }

    private boolean isExcludedToken(String token) {
        return parameterMappingUtils.isExcludedToken(token);
    }
}
