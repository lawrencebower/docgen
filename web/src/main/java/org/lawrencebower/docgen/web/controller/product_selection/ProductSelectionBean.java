package org.lawrencebower.docgen.web.controller.product_selection;

import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.binding.ProductBindBean;
import org.springframework.util.AutoPopulatingList;

import java.util.List;

public class ProductSelectionBean {

    private List<ProductBindBean> products;

    public ProductSelectionBean() {
        init();
    }

    public void init() {
        products = new AutoPopulatingList<>(ProductBindBean.class);
    }

    public List<ProductBindBean> getProducts() {
        return products;
    }

    public void setProducts(List<ProductBindBean> products) {
        this.products.addAll(products);
    }

    public void setProductViews(List<ProductView> productViews) {

        this.products.clear();

        for (ProductView productView : productViews) {

            ProductBindBean productBindBean = new ProductBindBean();

            String id = productView.getProductId();
            String quantityString = productView.getQuantityString();
            String productValue = productView.getProductValue();
            String productName = productView.getProductName();

            productBindBean.setProductId(id);
            productBindBean.setQuantity(quantityString);
            productBindBean.setValue(productValue);
            productBindBean.setProductName(productName);

            products.add(productBindBean);
        }
    }

}
