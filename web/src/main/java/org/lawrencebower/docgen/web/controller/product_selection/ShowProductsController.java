package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;

@Controller
@Scope("session")
public class ShowProductsController {

    static Logger logger = Logger.getLogger(ShowProductsController.class);

    private ProductSelectionCB business;

    @Autowired
    protected void setBusiness(ProductSelectionCB business) {
        this.business = business;
    }

    @RequestMapping("/productSelect")
    public String showProducts(Model model){

        List<ProductView> products = business.getProducts();

        model.addAttribute("products", products);

        return "products";
    }
}