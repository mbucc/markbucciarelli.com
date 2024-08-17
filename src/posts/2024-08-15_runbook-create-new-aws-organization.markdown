August 15, 2024

tags: runbook aws


runbook: Create new AWS Organization
===============================

Use AWS Control Tower to add a new organization unit (OU) and admin account
to an existing organization.

1. **LOGIN** to your AWS identity center.
   
2. **CLICK** the admin role ("AWSAdministratorAccess") for the management account.

3. **OPEN** the Control Tower service.

4. **CLICK** "Organization" in the vertical menu on the left.

5. **CLICK** the "Create resources" button.

6. **SELECT** "Create organizational unit" in the context menu.

7. **ENTER** the new OU name, the parent, and **CLICK** the "Add" button.

8. **WAIT** for the new OU to be created.

9. **CLICK** "Account factory" in the vertical menu on the left.

10. **CLICK** the "Create account" button.

11. **ENTER** the account email, using the pattern aws+{{env}}-{{OU name}}@example.com.
For example, if you are adding the OU `foo` to under `root/workloads/prod`, then
the account email would be aws+prod-foo@example.com.

12. **ENTER** the email of an existing identity center user to add
this role to the list they can chose from when they login.  For
example, aws+{{user-name}}@example.com.


13. **ENTER** the first and last name for this user.

14. **PICK** the OU that you just created as the "Organizational unit".

15. **CLICK** the "Create account" button.

16. **WAIT** for AWS to create the account.

17. **VERIFY** you see the new OU and the AWSAdministratorAccess role on the 
identity center AWS access portal.
