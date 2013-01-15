package org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.product;

import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.Map;

public interface ProductMapper {

    void mapFieldValuesToProduct(Map<String, String[]> parameterMap,
                                 ProductView productView);
}