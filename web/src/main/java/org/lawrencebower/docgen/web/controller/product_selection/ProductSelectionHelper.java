package org.lawrencebower.docgen.web.controller.product_selection;

import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;

import java.util.List;

public class ProductSelectionHelper {

    @Autowired
    private ProductSelectionCB business;

    public void putAllProductsOnModel(Model model) {
        List<ProductView> products = business.getProducts();
        model.addAttribute("products", products);
        model.addAttribute("productSelection", new ProductSelectionBean());
        model.addAttribute("noProductId", ProductSelectionController.NULL_PRODUCT_ID);
    }
}
