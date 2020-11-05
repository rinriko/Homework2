Sidebar <- function() {
  bs4DashSidebar(
       skin = "dark",
       bs4SidebarMenu(
         id = "sidebar_menu",
         bs4SidebarMenuItem(
           tabName = "pca",
           text = "PCA",
           icon = "chart-bar"
         ),
         bs4SidebarMenuItem(
           tabName = "eig_face",
           text = "Eigen faces",
           icon = "grin-beam"
         ),
         bs4SidebarMenuItem(
           tabName = "document",
           text = "Document",
           icon = "file"
         )
       )
     )
}