package org.lawrencebower.docgen.web.controller.customer_selection;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web_logic.business.controler_business.customer_selection.CustomerSelectionCB;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;

@Controller
@Scope("session")
public class ReloadCustomersController {

    static Logger logger = Logger.getLogger(ReloadCustomersController.class);

    private CustomerSelectionCB business;

    @Autowired
    public void setBusiness(CustomerSelectionCB business) {
        this.business = business;
    }

    @RequestMapping("/reloadCustomers")
    public String reloadCustomers(Model model) {

        business.reloadCustomers();

        List<ContactView> customers = business.getCustomers();

        model.addAttribute("customers", customers);

        return "home";
    }

}
