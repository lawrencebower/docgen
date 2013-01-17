package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
@Scope("session")
public class ProductSelectionController {

    static Logger logger = Logger.getLogger(ProductSelectionController.class);

    private ProductSelectionCB business;
    private ProductSelectionHelper productHelper;
    private SessionData sessionData;

    public static final String NULL_PRODUCT_ID = "NO_PRODUCT";

    @Autowired
    protected void setBusiness(ProductSelectionCB business) {
        this.business = business;
    }

    @Autowired
    public void setProductHelper(ProductSelectionHelper productHelper) {
        this.productHelper = productHelper;
    }

    @Autowired
    protected void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    @RequestMapping(value = "/productSelect", method = RequestMethod.POST)
    public String selectProduct(ProductSelectionBean productSelection,
                                Model model) {

        String productId = productSelection.getProductId();

        logger.error("productId = [" + productId + "]");

        getProductAndAddToSession(productId);

        putAllProductsOnPageModel(model);

        return "products";
    }

    public void putAllProductsOnPageModel(Model model) {
        productHelper.putAllProductsOnModel(model);
    }

    private void getProductAndAddToSession(String productId) {
        if (!NULL_PRODUCT_ID.equals(productId)) {
            ProductView selectedProduct = business.getProduct(productId);
            sessionData.addSelectedProduct(selectedProduct);
        }
    }
}