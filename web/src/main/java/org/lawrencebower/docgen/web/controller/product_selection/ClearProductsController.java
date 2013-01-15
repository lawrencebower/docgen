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

import java.util.List;

@Controller
@Scope("session")
public class ClearProductsController {

    static Logger logger = Logger.getLogger(ClearProductsController.class);

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

    @RequestMapping("/productSelect/clearProducts")
    public String clearProducts(Model model){

        sessionData.clearSelectedProducts();

        List<ProductView> products = business.getProducts();

        model.addAttribute("products", products);
        model.addAttribute("productSelection", new ProductSelectionBean());

        return "products";
    }
}