####LEEME####

Pasos para editar el GitHub y realizar cambios a los archivos. 

1- Descargar Git para tu sistema operativo (probablemente x64) de la página: https://git-scm.com/. En esta página verás un botón que dice "Latest Source Release". Adicionalmente, debes crear una cuenta de GitHub, si es que no tienes. 

2- Una vez lo descargues debes buscar en tu computador Windows PowerShell (que es el terminal) y en el cuadro de texto vas a poner lo siguiente:
    
    git clone https://github.com/plutoneraplaneta/Proyecto-de-Datos-Abiertos-sobre-Rendimiento-Academico-DARA-
    
  Cuando este OK lo cierras y abres RStudio.
    
    
3- Con RStudio abierto debes abrir el proyecto desde GitHub. Para esto necesitas ir a: File -> New Proyect... -> Version Control -> Git 

4- Una vez aquí debes poner la url del GitHub (la de git clone), luego, en directory name debes poner el nombre que quieres ponerle al proyecto. Finalmente, debes introducir donde quieres guardar el proyecto en tu computador. 

5- Una vez lo hagas tendras el proyecto desde GitHub. Ojo, las bases de datos no fueron incluidas en el GitHub. Para descargarlas debes acceder al siguiente link que te compartí con tu correo UC:       
(https://uccl0-my.sharepoint.com/:u:/g/personal/liam_obilinovic_uc_cl/Ec4VPyWewCpEoveA6dYNLKsB7l50mn8j6jRfNxLGcJ6xxg?email=ignacio.serrano%40uc.cl&e=XBCgaC). Asegurate de extraer este RAR en la carpeta del proyecto. 

6- Finalmente, debes ir a Tools -> Version Control -> Proyect Setup... y en Version Control System debes poner Git y reiniciar R. (Todo esto suponiendo si es que en Version Control System dice None. En caso de que diga Git no hacer nada)

7- Ahora, ya puedes trabajar en los scripts, así como crear nuevos, borrar, etc. Todo esto será compartido entre ambos. ESO SI, TIENES QUE ASEGURARTE DE GUARDAR ESTOS CAMBIOS EN EL GITHUB DE LA SIGUIENTE MANERA:

  -Para guardar los cambios debes ir al panel superior derecho y presionar la pestaña Git
  -Una vez aquí verás que están los archivos el proyecto. Imaginemos que creaste un archivo llamado "Hola.r". Verás al lado de ese archivo un check el   -cual debes chequear. Una vez lo chequees debes presionar Commit. 
  -se abrirá una interfaz en la cual puedes comentar los cambios que has realizado a los archivos. No es necesario que digas todo pero por ulitmo
  decir que es lo que pasó. Una vez lo hagas debes darle al boton PUSH. Esto hará cambios en el GitHub. 
  -Finalmente, te pedirá tu usuario de GitHub y luego la contraseña la cual es la que te mandé. Solo puedes usar esa. Una vez hagas eso ya estarían     todos los cambios listos. 
  



