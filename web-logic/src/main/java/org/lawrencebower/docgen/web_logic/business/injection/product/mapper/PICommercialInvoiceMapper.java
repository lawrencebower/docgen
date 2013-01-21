package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public class PICommercialInvoiceMapper extends PIAbstractMapper {

    private PICommercialInvoiceMapper() {
        super(ProductInjectionField.PRODUCT_COMMERCIAL_INVOICE_DESCRIPTION);//force spring creation
    }

    @Override
    protected String getValue(ProductView product) {
        return product.getCommercialInvoiceDescription();
    }
}
