package org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.product;

import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.binding.ProductBindBean;

import java.util.List;

public interface ProductMapper {

    void mapToProducts(List<ProductBindBean> productBindBeans,
                       ProductView productView);
}
