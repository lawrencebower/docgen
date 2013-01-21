package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public class PINameAndDescriptionMapper extends PIAbstractMapper {

    private PINameAndDescriptionMapper() {
        super(ProductInjectionField.PRODUCT_NAME_AND_DESCRIPTION);//force spring creation
    }

    @Override
    protected String getValue(ProductView product) {
        String productId = product.getProductId();
        String productName = product.getProductName();
        return productId + " - " + productName;
    }
}
