package org.lawrencebower.docgen.web_logic.business.injection.product;

import org.lawrencebower.docgen.web_model.view.product.ProductView;

public interface ProductInjectionMapper {

    String getProductFieldByType(ProductInjectionField productField, ProductView product);

}
