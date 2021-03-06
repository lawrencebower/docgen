<%@ include file="/WEB-INF/jsp/include.jsp" %>

Select customer

<jsp:useBean id="customers"
             scope="request"
             type="java.util.List<org.lawrencebower.docgen.web_model.view.contact.ContactView>"/>

<div>
    <ol>
        <table border="1">
            <tr>
                <td>Contact
                <td>
                    <%--<td>Company</td>--%>
            </tr>
            <c:forEach var="customer" items="${customers}">
                <tr>
                    <td>
                        <abbr title="${customer.HTMLAddress}&nbsp;${customer.country}"><c:out value="${customer.contactName}"/> - <c:out value="${customer.name}"/></abbr>
                        <%--<c:out value="${customer.HTMLAddress}" escapeXml="false"/><br/>--%>
                        <%--<c:out value="${customer.country}"/>--%>
                    </td>
                        <%--<td>--%>
                        <%--<c:set var="contact" value="${customer.contact}" scope="page"/>--%>
                        <%--<c:out value="${contact.name}"/><br/>--%>
                        <%--<c:out value="${contact.contactName}"/><br/>--%>
                        <%--<c:out value="${contact.HTMLAddress}"/><br/>--%>
                        <%--<c:out value="${contact.country}"/>--%>
                        <%--</td>--%>
                    <td>
                        <s:url var="customer_url" value="/customerSelect/customerName/{customerId}">
                            <s:param name="customerId" value="${customer.contactId}"/>
                        </s:url>
                        <a href="${customer_url}">
                            select
                        </a>
                    </td>
                </tr>
            </c:forEach>
        </table>

    </ol>

    <s:url var="reload_url" value="/reloadCustomers"/>
    <a href="${reload_url}">reload customers</a>
</div>
