package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.List;

@Controller
@Scope("session")
public class ProductSelectionController {

    static Logger logger = Logger.getLogger(ProductSelectionController.class);

    private ProductSelectionCB business;
    private SessionData sessionData;

    @Autowired
    protected void setBusiness(ProductSelectionCB business) {
        this.business = business;
    }

    @Autowired
    protected void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    @RequestMapping(value = "/productSelect/productId/{productId}", method = RequestMethod.GET)
    public String selectProduct(@PathVariable String productId,
                                Model model) {

        logger.error("productId = [" + productId + "]");

        getProductAndAddToSession(productId);

        putAllProductsOnPageModel(model);

        return "products";
    }

    private void putAllProductsOnPageModel(Model model) {
        List<ProductView> products = business.getProducts();
        model.addAttribute("products", products);
    }

    private void getProductAndAddToSession(String productId) {
        ProductView selectedProduct = business.getProduct(productId);
        sessionData.addSelectedProduct(selectedProduct);
    }
}