package org.lawrencebower.docgen.web.controller.product_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web_logic.business.controler_business.product_selection.ProductSelectionCB;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

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

    @RequestMapping(value = "/productSelect", method = RequestMethod.GET)
    public String showProducts(Model model) {

        List<ProductView> products = business.getProducts();

        model.addAttribute("products", products);
        model.addAttribute("productSelection", new ProductSelectionBean());

        return "products";
    }
}