package org.lawrencebower.docgen.web.controller;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.CustomerSelectionCB;
import org.lawrencebower.docgen.web_model.view.business.ContactView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
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
@RequestMapping("/customerSelect")
public class CustomerSelectionController {

    static Logger logger = Logger.getLogger(CustomerSelectionController.class);

    @Autowired
    private CustomerSelectionCB business;
    @Autowired
    private SessionData sessionData;

    public CustomerSelectionController() {
    }

    @RequestMapping({"/", "/home"})
    public String showHomePage(Model model) {

        List<ContactView> customers = business.getCustomers();

        model.addAttribute("customers", customers);

        return "home";
    }

    @RequestMapping(value = "/customerName/{customerName}", method = RequestMethod.GET)
    public String selectCustomer(@PathVariable String customerName,
                                 Model model) {

        logger.error("customerName = [" + customerName + "]");

        ContactView selectedCustomer = business.getCustomer(customerName);
        sessionData.setSelectedCustomer(selectedCustomer);

        ContactView selectedBusiness = business.getBusinessByCustomerName(customerName);
        sessionData.setSelectedBusiness(selectedBusiness);

        List<ProductView> products = business.getProducts();

        model.addAttribute("products", products);

        return "products";
    }
}
