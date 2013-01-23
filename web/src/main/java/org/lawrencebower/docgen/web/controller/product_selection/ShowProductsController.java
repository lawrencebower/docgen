package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
@Scope("session")
public class ShowProductsController {

    static Logger logger = Logger.getLogger(ShowProductsController.class);

    @Autowired
    private SessionData sessionData;

    private ProductSelectionHelper productHelper;

    @Autowired
    public void setProductHelper(ProductSelectionHelper productHelper) {
        this.productHelper = productHelper;
    }

    @RequestMapping(value = "/productSelect", method = RequestMethod.GET)
    public String showProducts(Model model) {

        productHelper.putProductsOnModel(model, sessionData);

        return "products";
    }
}