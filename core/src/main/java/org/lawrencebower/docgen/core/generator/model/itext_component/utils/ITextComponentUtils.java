package org.lawrencebower.docgen.core.generator.model.itext_component.utils;

import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;

import java.util.List;

public class ITextComponentUtils {

    public void checkCoordinatesPresent(List<ITextComponent> components) {
        for (ITextComponent component : components) {
            checkCoordinatesPresent(component);
        }
    }

    public void checkCoordinatesPresent(ITextComponent component) {
        DocCoordinates coordinates = component.getCoordinates();
        if (coordinates == null) {
            String componentName = component.getName();
            throw new DocGenException("Coordinates are null for component " + componentName);
        }
    }

}
