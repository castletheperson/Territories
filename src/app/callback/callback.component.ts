import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { AuthService } from '../auth/auth.service';

@Component({
  selector: 'app-callback',
  templateUrl: './callback.component.html',
  styleUrls: ['./callback.component.css']
})
export class CallbackComponent {

  constructor(private router: Router, private auth: AuthService) {

    auth.handleAuthentication(() => {
      const referToUrl = localStorage.getItem('referTo') || '/';
      localStorage.removeItem('referTo');
      router.navigateByUrl(referToUrl);
    });
  }

}
