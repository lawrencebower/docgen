package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.product;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.ParameterMappingUtils;
import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.product.ProductMapper;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Map;

public class ProductMapperImpl implements ProductMapper {

    @Autowired
    ParameterMappingUtils parameterMappingUtils;

    @Override
    public void mapFieldValuesToProduct(Map<String, String[]> parameterMap,
                                        ProductView productView) {

        for (String key : parameterMap.keySet()) {

            if (isExcludedToken(key)) {
                continue;
            }

            extractAndSetValues(parameterMap,
                                productView,
                                key);
        }
    }

    private void extractAndSetValues(Map<String, String[]> parameterMap,
                                     ProductView productView,
                                     String productParamName) {

        String parameterValue = getParameterValue(productParamName, parameterMap);

        ProductParameterInfo paramInfo = extractParameterInfo(productParamName, parameterValue);

        paramInfo.setProductFieldIfMatch(productView);
    }

    private ProductParameterInfo extractParameterInfo(String paramName, String paramValue) {

        String productId = extractProductIdFromParam(paramName);

        ViewConstants.PRODUCT_TOKEN_TYPE paramType = extractTypeFromParam(paramName);

        return new ProductParameterInfo(productId, paramType, paramValue);
    }

    private ViewConstants.PRODUCT_TOKEN_TYPE extractTypeFromParam(String value) {

        String[] tokens = parameterMappingUtils.splitStringBySeparator(value);

        checkParamValueTokenLength(value, tokens);

        return ViewConstants.PRODUCT_TOKEN_TYPE.mapFromString(tokens[1]);
    }

    private void checkParamValueTokenLength(String value, String[] tokens) {
        if(tokens.length != 2){
            String message = String.format(
                    "Parameter value '%s' does not have 2 tokens, the product id and the token type?!", value);
            throw new DocGenException(message);
        }
    }

    private String extractProductIdFromParam(String value) {

        String[] tokens = parameterMappingUtils.splitStringBySeparator(value);

        checkParamValueTokenLength(value, tokens);

        return tokens[0];
    }

    private String getParameterValue(String key, Map<String, String[]> parameterMap) {
        return parameterMappingUtils.getParameterValue(key, parameterMap);
    }

    private boolean isExcludedToken(String token) {
        return parameterMappingUtils.isExcludedToken(token);
    }

    private class ProductParameterInfo {

        private String productId;
        private ViewConstants.PRODUCT_TOKEN_TYPE productField;
        private String productFieldValue;

        private ProductParameterInfo(String productId,
                                     ViewConstants.PRODUCT_TOKEN_TYPE productField,
                                     String productFieldValue) {
            this.productId = productId;
            this.productField = productField;
            this.productFieldValue = productFieldValue;
        }

        public void setProductFieldIfMatch(ProductView productView) {

            if (productView.idMatches(productId)) {

                switch (productField) {
                    case VALUE:
                        productView.setValue(productFieldValue);
                        break;
                    case QUANTITY:
                        productView.setQuantity(productFieldValue);
                        break;
                    default:
                        String message = String.format("Product field name '%s' not recognised?!", productField);
                        throw new DocGenException(message);
                }
            }
        }
    }
}
