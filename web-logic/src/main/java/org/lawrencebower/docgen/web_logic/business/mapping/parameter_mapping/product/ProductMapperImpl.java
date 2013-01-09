package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.product;

import org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.ParameterMappingUtils;
import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.product.ProductMapper;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.Map;

public class ProductMapperImpl implements ProductMapper {

    @Autowired
    ParameterMappingUtils parameterMappingUtils;

    @Override
    public void mapFieldValuesToComponents(Map<String, String[]> parameterMap,
                                           List<ProductView> allProducts) {

        for (String key : parameterMap.keySet()) {

            if (isExcludedToken(key)) {
                continue;
            }

            extractAndSetValues(parameterMap,
                                allProducts,
                                key);
        }
    }

    private void extractAndSetValues(Map<String, String[]> parameterMap,
                                     List<ProductView> allViewComponents,
                                     String componentName) {

        String componentValue = getParameterValue(componentName, parameterMap);

        for (ProductView product : allViewComponents) {
        }
    }

    private String getParameterValue(String key, Map<String, String[]> parameterMap) {
        return parameterMappingUtils.getParameterValue(key, parameterMap);
    }

    private boolean isExcludedToken(String token) {
        return parameterMappingUtils.isExcludedToken(token);
    }
}
