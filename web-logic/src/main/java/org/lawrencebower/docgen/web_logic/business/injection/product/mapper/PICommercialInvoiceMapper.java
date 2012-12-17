package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionMapper;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

public class PICommercialInvoiceMapper implements ProductInjectionMapper {

    private ProductInjectionField piField = ProductInjectionField.PRODUCT_COMMERCIAL_INVOICE_DESCRIPTION;

    private PICommercialInvoiceMapper() {
        //force spring creation
    }

    @Override
    public String getProductFieldByType(ProductInjectionField productField, ProductView product) {

        String value = ProductInjectionMapperImpl.EMPTY_STRING;

        if(piField == productField){
            value = product.getCommercialInvoiceDescription();
        }

        return value;
    }
}
