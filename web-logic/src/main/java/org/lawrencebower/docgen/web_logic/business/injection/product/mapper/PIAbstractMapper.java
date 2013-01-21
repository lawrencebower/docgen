package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionMapper;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

public abstract class PIAbstractMapper implements ProductInjectionMapper {

    protected ProductInjectionField piField;

    protected PIAbstractMapper(ProductInjectionField piField) {
        this.piField = piField;
    }

    @Override
    public String getProductFieldByType(ProductInjectionField productField, ProductView product) {

        String value = ProductInjectionMapperImpl.EMPTY_STRING;

        if (piField == productField) {
            value = getValue(product);
        }

        return value;
    }

    protected abstract String getValue(ProductView product);
}