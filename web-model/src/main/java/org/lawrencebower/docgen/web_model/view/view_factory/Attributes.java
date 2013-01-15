package org.lawrencebower.docgen.web_model.view.view_factory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Attributes {

    private List<String> attributes = new ArrayList<>();

    public Attributes(String... attributes) {
        this.attributes = Arrays.asList(attributes);
    }

    /**
     * This attributes is regarded as the 'master' when determining a match. If this set does not specify
     * any attributes, and compareTo attribute will be considered a match. If this attributes does contain
     * one or more values, at least one of the values must match in the compareTo attributes to match
     */
    public boolean isAttributeMatch(Attributes compareTo){

        boolean isMatch = false;

        if(isEmpty()){
            isMatch = true;
        }

        for (String attribute : attributes) {
            if(compareTo.isAttributeMatch(attribute)){
                isMatch = true;
                break;
            }
        }

        return isMatch;
    }

    public boolean isAttributeMatch(String attribute){
        return attributes.contains(attribute);
    }

    public boolean isEmpty() {
        return attributes.isEmpty();
    }

}
