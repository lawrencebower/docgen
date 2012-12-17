package org.lawrencebower.docgen.web_logic.business.injection.product.mapper;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionMapper;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.ArrayList;
import java.util.List;

public class ProductInjectionMapperImpl implements ProductInjectionMapper {

    private List<ProductInjectionMapper> mappers = new ArrayList<>();

    protected static final String EMPTY_STRING = "";

    @Override
    public String getProductFieldByType(ProductInjectionField productField, ProductView product) {

        for (ProductInjectionMapper mapper : mappers) {

            String result = mapper.getProductFieldByType(productField, product);

            if (result.isEmpty()) {
                continue;//continue until match found
            }

            return result;
        }

        String message = String.format("ProductInjectionField '%s' not mapped", productField);
        throw new DocGenException(message);
    }

    public void setNameMapper(PINameMapper mapper) {
        mappers.add(mapper);
    }

    public void setNameAndDescriptionMapper(PINameAndDescriptionMapper mapper) {
        mappers.add(mapper);
    }

    public void setCommercialInvoiceMapper(PICommercialInvoiceMapper mapper) {
        mappers.add(mapper);
    }

    public void setHarmonizedTariffMapper(PIHarmonizedTariffMapper mapper) {
        mappers.add(mapper);
    }

    public void setOriginMapper(PIOriginMapper mapper) {
        mappers.add(mapper);
    }

    public void setQuantityMapper(PIQuantityMapper mapper) {
        mappers.add(mapper);
    }

    public void setValueMapper(PIValueMapper mapper) {
        mappers.add(mapper);
    }
}
