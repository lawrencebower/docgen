package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public class PIOriginMapper extends PIAbstractMapper{

    private PIOriginMapper() {
        super(ProductInjectionField.PRODUCT_ORIGIN);
    }

    @Override
    protected String getValue(ProductView product) {
        return product.getProductCountryOfOrigin();
    }
}
