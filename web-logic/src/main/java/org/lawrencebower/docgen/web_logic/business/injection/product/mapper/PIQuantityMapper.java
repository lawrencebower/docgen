package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public class PIQuantityMapper extends PIAbstractMapper {

    private PIQuantityMapper() {
        super(ProductInjectionField.PRODUCT_QUANTITY);
    }

    @Override
    protected String getValue(ProductView product) {
        return product.getQuantityString();
    }
}
