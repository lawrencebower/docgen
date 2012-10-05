package org.lawrencebower.docgen.web.controller;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.xml_mapper.ModelFactory;
import org.lawrencebower.docgen.web_logic.view.customer.CustomerView;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
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
@RequestMapping("/customerSelect")
public class CustomerSelectionController {

    static Logger logger = Logger.getLogger(CustomerSelectionController.class);

    @Autowired
    private ModelFactory xmlModelMapper;
    @Autowired
    private SessionData sessionData;

    public CustomerSelectionController() {
    }

    @RequestMapping({"/", "/home"})
    public String showHomePage(Model model) {

        List<CustomerView> customers = xmlModelMapper.getCustomers();

        model.addAttribute("customers", customers);

        return "home";
    }

    @RequestMapping(value = "/customerName/{customerName}", method = RequestMethod.GET)
    public String selectCustomer(@PathVariable String customerName,
                                 Model model) {

        logger.error("customerName = [" + customerName + "]");

        CustomerView selectedCustomer = xmlModelMapper.getCustomer(customerName);
        sessionData.setSelectedCustomer(selectedCustomer);

        List<ProductView> products = xmlModelMapper.getProducts();

        model.addAttribute("products", products);

        return "products";
    }
}
