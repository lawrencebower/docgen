package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.product;

import org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.ParameterMappingUtils;
import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.product.ProductMapper;
import org.lawrencebower.docgen.web_model.view.product.ProductBindBean;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class ProductMapperImpl implements ProductMapper {

    @Autowired
    ParameterMappingUtils parameterMappingUtils;

    @Override
    public void mapToProducts(List<ProductBindBean> productBindBeans,
                              ProductView productView) {

        for (ProductBindBean productBindBean : productBindBeans) {

            extractAndSetValues(productView,
                                productBindBean);
        }
    }

    private void extractAndSetValues(ProductView productView,
                                     ProductBindBean productBindData) {

        String bindId = productBindData.getProductId();
        String productId = productView.getProductId();

        if(bindId.equals(productId)){
            String bindValue = productBindData.getValue();
            String bindQuantity = productBindData.getQuantity();

            productView.setValue(bindValue);
            productView.setQuantity(bindQuantity);
        }
    }
}
