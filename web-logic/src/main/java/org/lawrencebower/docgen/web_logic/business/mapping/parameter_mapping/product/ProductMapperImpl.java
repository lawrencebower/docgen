package org.lawrencebower.docgen.web_logic.business.mapping.parameter_mapping.product;

import org.lawrencebower.docgen.web_model.business_def.mapping.parameter_mapping.product.ProductMapper;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.binding.ProductBindBean;

import java.util.List;

public class ProductMapperImpl implements ProductMapper {

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

        productView.checkAndSetValuesFromBindBean(productBindData);
            }
}
