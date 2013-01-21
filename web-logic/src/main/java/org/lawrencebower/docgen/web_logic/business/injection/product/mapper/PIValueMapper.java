package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public class PIValueMapper extends PIAbstractMapper {

    private PIValueMapper() {
        super(ProductInjectionField.PRODUCT_VALUE);
    }

    @Override
    protected String getValue(ProductView product) {
        return product.getProductValue();
    }
}
