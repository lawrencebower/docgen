<%@ include file="/WEB-INF/jsp/include.jsp" %>

Select customer

<jsp:useBean id="customers" scope="request"
             type="java.util.List<org.lawrencebower.docgen.web_logic.view.contact.ContactView>"/>

<div>
    <ol>
        <table border="1">
            <tr><td>Contact<td>Company<td></td></tr>
            <c:forEach var="product" items="${customers}">
                <tr>
                    <td>
                        <c:out value="${product.name}"/><br/>
                        <c:out value="${product.contactName}"/><br/>
                        <c:out value="${product.address}"/>
                    </td>
                    <td>
                        <c:set var="contact" value="${product.contact}" scope="page"/>
                        <c:out value="${contact.name}"/><br/>
                        <c:out value="${contact.contactName}"/><br/>
                        <c:out value="${contact.address}"/>
                    </td>
                    <td>
                        <s:url var="customer_url" value="/customerSelect/customerName/{customerName}">
                            <s:param name="customerName" value="${product.name}"/>
                        </s:url>
                        <a href="${customer_url}">
                            select
                        </a>
                    </td>
                </tr>
            </c:forEach>
        </table>
    </ol>
</div>
