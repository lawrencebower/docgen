package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@Scope("session")
public class ClearProductsController {

    static Logger logger = Logger.getLogger(ClearProductsController.class);

    private ProductSelectionHelper productHelper;
    private SessionData sessionData;

    @Autowired
    protected void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    @Autowired
    public void setProductHelper(ProductSelectionHelper productHelper) {
        this.productHelper = productHelper;
    }

    @RequestMapping("/productSelect/clearProducts")
    public String clearProducts(Model model){

        sessionData.clearSelectedProducts();

        productHelper.putAllProductsOnModel(model, sessionData);

        return "products";
    }
}