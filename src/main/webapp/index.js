var Plotter = function( element, w, h ) {
    
	this.canvas_element = element;
	this.width = w;
	this.height = h;

	this.renderer = null;
	this.scene = null;
	this.camera = null;
	this.light = null;
	this.particles = null;

    this.camera_r = 250;
	this.camera_phi = 90;
	this.camera_theta = 0;
    
    this.camera_r_flag = 1;
    
/*	this.controls = null; */
};

Plotter.prototype = {

	plotStart : function() {

        this.initThree();
		this.initScene();
		this.initCamera();
		this.initLight();
		this.initObject();

/*		this.plotLoop(); */
	},


	initThree : function() {
		this.renderer = new THREE.WebGLRenderer( { antialias : true } );
		this.renderer.sortObjects = false;
		this.renderer.setSize( this.width, this.height );
		this.canvas_element.append( this.renderer.domElement );
		this.renderer.setClearColorHex( 0xFFFFFF, 1.0 );
	},


	initScene : function() {
		this.scene = new THREE.Scene();
	},


	initCamera : function() {
		this.camera = new THREE.PerspectiveCamera( 45, this.width / this.height, 0.1, 10000 );

        var pos = this.getCameraPosition();
		this.camera.position.set( pos.x, pos.y, pos.z );
		this.camera.up.set( 0, 0, 1 );
		this.camera.lookAt( { x : 0, y : 0, z : 0 } );
/*
		this.controls = new THREE.TrackballControls( this.camera );
		this.controls.rotateSpeed = 1.0;
		this.controls.zoomSpeed = 1.2;
		this.controls.panSpeed = 0.8;
		this.controls.noZoom = false;
		this.controls.noPan = false;
		this.controls.staticMoving = true;
		this.controls.dynamicDampingFactor = 0.3;
*/
	},


	initLight : function() {
		this.light = {};
		this.light[0] = new THREE.DirectionalLight( 0xFFFFFF, 1.0, 0 ); // 平行光源
		this.light[0].position.set( 0, 0, 1000 );
		this.scene.add( this.light[0] );
		
		var ambient_light = new THREE.AmbientLight( 0xCCCCCC ); // 環境光源
		this.scene.add( ambient_light );
	},



	initObject : function() {
	},


	getCameraPosition : function() {
		
		var rad_phi = this.camera_phi * Math.PI / 180.0;
		var rad_theta = this.camera_theta * Math.PI / 180.0;
		
		result = new THREE.Vector3( 0, 0, 0 );
		result.x = this.camera_r * Math.sin( rad_phi ) * Math.cos( rad_theta );
		result.y = this.camera_r * Math.sin( rad_phi ) * Math.sin( rad_theta );
		result.z = this.camera_r * Math.cos( rad_phi );
		return result;
	},
    
    
	plotLoop : function() {

/*		this.controls.update(); */


        this.camera_r += this.camera_r_flag;
        if ( this.camera_r > 500 ) {
            this.camera_r_flag = -1;
        } else if ( this.camera_r <= 0 ) {
            this.camera_r_flag = 1;
        }
        
        this.camera_theta += 0.1;
        
        var pos = this.getCameraPosition();
        this.camera.position.set( pos.x, pos.y, pos.z );
		this.camera.lookAt( { x : 0, y : 0, z : 0 } );
        
		this.renderer.clear();
		this.renderer.render( this.scene, this.camera );
        var plotter = this;
		window.requestAnimationFrame( function() { plotter.plotLoop(); } );
	},
    
    
    createParticles : function( obj ) {
		
        if ( this.particles !== null ) {
            this.removeParticles();
        }
        
		var image_width = obj.width;
		var image_height = obj.height;
		var color_max = obj.color_max;
		var colors = obj.colors;
		
		var offset_width = image_width / 2;
		var offset_height = image_height / 2;
		
		var geometry = new THREE.Geometry();
		
		for ( var i = 0, l = colors.length; i < l; i++ ) {
			
			var data = colors[i];
			
			var vertex = new THREE.Vector3( data.x - offset_width, 0, - ( data.y - offset_height ) );
			geometry.vertices.push( vertex );
			
			var color = new THREE.Color( 0xFFFFFF );
			color.setRGB( data.red / color_max, data.green / color_max, data.blue / color_max );
			geometry.colors.push( color );
		}
		
		var material = new THREE.ParticleBasicMaterial( {
			size : 5,
			blending : THREE.AdditiveBlending,
			depthTest : false,
			vertexColors : true
		} );
		
		this.particles = new THREE.ParticleSystem( geometry, material );
		this.particles.position.set( 0, 0, 0 );
		this.particles.sortParticles = false;
		this.scene.add( this.particles );		
	},
    
    
    removeParticles : function() {
        this.scene.remove( this.particles );
        this.particles = null;
    },

	
	onResize : function( width, height ) {

		this.width = width;
		this.height = height;

		this.renderer.setSize( this.width, this.height );
		this.camera.aspect = this.width / this.height;
		this.camera.updateProjectionMatrix();
	}
};


var PPMReader = function() {
	
};


PPMReader.prototype = {

		/*
		 * PPM ファイルのデータを読み込み、Object 型に変換して返す。
		 * Object の中身は次の通り : 
		 *    int width : 画像の幅
		 *    int height : 画像の高さ
		 *    int color_max : 色最大値
		 *    Object colors : 各ピクセルの色
		 *       - x
		 *       - y
		 *       - red
		 *       - green
		 *       - blue
		 * 
		 * 読めなかった場合は null を返す。
		 */
		read : function( lines ) {
			
			var result = {};
			var result_colors = [];
			
			var image_width = 0;
			var image_height = 0;
			var color_max = 0;
			
			var line_count = 0;
			var color_count = 0;
			var rgb_count = 0;
			
			var red = 0;
			var green = 0;
			var blue = 0;
			
			for ( var i = 0, l = lines.length; i < l; i++ ) {

				var line = lines[i];
                var data = "";

				// 空行　or コメント行は読まない
				if ( line.length === 0 || line[0] === '#' ) {
					continue;
				}
				line_count += 1;
				
				if ( line_count == 1 ) {

					// 1行目は「P3」
					if ( line != 'P3' ) {
						return null;
					}

				} else if ( line_count == 2 ) {

					// 2行目は画像の幅と高さ
					data = line.split( ' ' );
					if ( data.length < 2 ) {
						return null;
					}						
					image_width = parseInt( data[0], 10 );
					image_height = parseInt( data[1], 10 );

				} else if ( line_count == 3 ) {

					// 3行目は色の最大値
					color_max = parseInt( line, 10 );

				} else {

					// 残りは画像の色
					data = line.split( ' ' );
					for ( var j = 0, dl = data.length; j < dl; j++ ) {
						rgb_count++;
						if ( rgb_count == 1 ) {
							// Red
							red = data[j];
						} else if ( rgb_count == 2 ) {
							// Green
							green = data[j];
						} else {
							// Blue
							blue = data[j];
							
							var r_obj = {};
							r_obj.red = parseInt( red, 10 );
							r_obj.green = parseInt( green, 10 );
							r_obj.blue = parseInt( blue, 10 );

							r_obj.x = color_count % image_width;
							r_obj.y = Math.floor( color_count / image_width );
							
							result_colors.push( r_obj );
							rgb_count = 0;
							color_count++;
						}
						
					}
				}
			}

			result.width = image_width;
			result.height = image_height;
			result.color_max = color_max;
			result.colors = result_colors;
			
			return result;
		}
		
};


$(document).ready( function() {

	var canvas_element = $('#canvas-frame');
	var width = canvas_element.width();
	var height = canvas_element.height();

	var plotter = new Plotter( canvas_element, width, height );
    var ppm_reader = new PPMReader();

	// Resize
	var resize_timer = false;
	$(window).resize( function() {
		if ( resize_timer !== false ) {
			clearTimeout( resize_timer );
		}
		resize_timer = setTimeout( function() {
			plotter.onResize( canvas_element.width(), canvas_element.height() );
		}, 200 );
	} );
    
    // File load
	$('#file').change( function() {

        if ( $(this).val().length === 0 ) {
            return;
        }
        
		$('#file-cover').val($(this).val());
        
		var file = this.files[0];
		var file_reader = new FileReader();
		file_reader.onload = function( event ) {

			var lines = event.target.result.split( '\n' );
			var objects = ppm_reader.read( lines );

			if ( objects === null ) {
				alert( 'ファイルを読み込めませんでした。' );
			} else {
				plotter.createParticles( objects );
			}
			
		};
		file_reader.readAsText( file, 'utf-8' );
	} );
	
	// Reset
	$('#reset').click( function() {
		plotter.removeParticles();
		$('#file-cover').val('');
        $('#file').val('');
	} );

   	plotter.plotStart();
});
