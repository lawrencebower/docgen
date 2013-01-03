package org.lawrencebower.docgen.web_model.view.product;

public abstract class ProductViewFactory {

    public ProductView createProductView(Product product){
        ProductView productView = createProductView();
        productView.setProduct(product);
        return productView;
    }

    public abstract ProductView createProductView();

}
