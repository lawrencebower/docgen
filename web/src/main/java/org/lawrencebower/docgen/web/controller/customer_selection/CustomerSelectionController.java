package org.lawrencebower.docgen.web.controller.customer_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.controller.product_selection.ProductSelectionBean;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.customer_selection.CustomerSelectionCB;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
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
public class CustomerSelectionController {

    static Logger logger = Logger.getLogger(CustomerSelectionController.class);

    private CustomerSelectionCB business;
    private SessionData sessionData;

    @Autowired
    public void setBusiness(CustomerSelectionCB business) {
        this.business = business;
    }

    @Autowired
    public void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    @RequestMapping(value = "/customerSelect/customerName/{customerId}", method = RequestMethod.GET)
    public String selectCustomer(@PathVariable String customerId,
                                 Model model) {

        logger.error("customerId = [" + customerId + "]");

        ContactView selectedCustomer = business.getCustomer(customerId);
        sessionData.setCustomerSelection(selectedCustomer);

        ContactView selectedBusiness = business.getBusinessByCustomerId(customerId);
        sessionData.setSelectedBusiness(selectedBusiness);

        List<ProductView> products = business.getProducts();

        model.addAttribute("products", products);
        model.addAttribute("productSelection", new ProductSelectionBean());

        return "products";
    }
}
