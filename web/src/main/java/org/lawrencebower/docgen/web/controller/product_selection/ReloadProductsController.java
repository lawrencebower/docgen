package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
@Scope("session")
public class ReloadProductsController {

    static Logger logger = Logger.getLogger(ReloadProductsController.class);

    @Autowired
    private ProductSelectionCB productSelectionBusiness;
    @Autowired
    private SessionData sessionData;

    private ProductSelectionHelper productHelper;

    @Autowired
    public void setProductHelper(ProductSelectionHelper productHelper) {
        this.productHelper = productHelper;
    }

    @RequestMapping(value = "/reloadProducts", method = RequestMethod.GET)
    public String reloadProducts(Model model) {

        productSelectionBusiness.reloadProducts();

        productHelper.putProductsOnModel(model, sessionData);

        return "products";
    }
}