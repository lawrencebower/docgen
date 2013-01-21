package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public class PIHarmonizedTariffMapper extends PIAbstractMapper {

    private PIHarmonizedTariffMapper() {
        super(ProductInjectionField.PRODUCT_HARMONIZED_TARIFF_DESCRIPTION);//force spring creation
    }

    @Override
    protected String getValue(ProductView product) {
        return product.getHarmonizedTariffNumber();
    }
}
