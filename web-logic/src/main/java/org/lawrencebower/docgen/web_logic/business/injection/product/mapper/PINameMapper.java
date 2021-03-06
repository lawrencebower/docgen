package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public class PINameMapper extends PIAbstractMapper {

    private PINameMapper() {
        super(ProductInjectionField.PRODUCT_NAME);
    }

    @Override
    protected String getValue(ProductView product) {
        return product.getProductName();
    }
}
