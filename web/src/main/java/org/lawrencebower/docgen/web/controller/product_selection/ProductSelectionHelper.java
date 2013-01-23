package org.lawrencebower.docgen.web.controller.product_selection;

import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.product.binding.ProductBindBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;

import java.util.List;

public class ProductSelectionHelper {

    @Autowired
    private ProductSelectionCB business;
    @Autowired
    private ProductSelectionBean productSelectionBean;

    public void putProductsOnModel(Model model,
                                   SessionData sessionData) {

        addAllProductsToModel(model);

        addSelectedProductsToModel(model, sessionData);

        model.addAttribute("productBindBean", new ProductBindBean());
        model.addAttribute("noProductId", ProductSelectionController.NULL_PRODUCT_ID);
    }

    private void addSelectedProductsToModel(Model model, SessionData sessionData) {
        List<ProductView> selectedProducts = sessionData.getSelectedProducts();
        productSelectionBean.setProductViews(selectedProducts);
        model.addAttribute("productSelectionBean", productSelectionBean);
    }

    private void addAllProductsToModel(Model model) {
        List<ProductView> products = business.getProducts();
        model.addAttribute("allProducts", products);
    }
}
